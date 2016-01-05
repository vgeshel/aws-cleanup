(ns aws-cleanup.core
  (:require [amazonica.aws.ec2 :as ec2])
  (:import [com.amazonaws AmazonServiceException]))

(defn get-snapshots [owner-ids]
  (-> (ec2/describe-snapshots {:owner-ids owner-ids})
      :snapshots))

(defn get-images [owner-ids]
  (-> (ec2/describe-images {:owners owner-ids})
      :images))

(defn get-image-ids [owner-ids]
  (->> (get-images owner-ids)
       (map :image-id)
       set))

(defn image-snapshot? [{:keys [description] :as snapshot}]
  (when-let [[_ image-id] (re-find #"Created by CreateImage\(i-[0-9a-z]+\) for (ami-[a-z0-9]+)" description)]
    (assoc snapshot :image-id image-id)))

(defn get-snapshots-with-image-ids
  "get snapshots that are part of AMIs and add image-id to the map"
  [owner-ids]
  (->> (get-snapshots owner-ids)
       (keep image-snapshot?)))

(defn find-orphaned-snapshots [owner-ids]
  (let [image-ids (get-image-ids owner-ids)
        snapshots (get-snapshots-with-image-ids owner-ids)]
    (remove (comp image-ids :image-id) snapshots)))

(defn delete-snapshot [snapshot-id]
  (try
    (ec2/delete-snapshot :snapshot-id snapshot-id)
    (printf "deleted %s\n" snapshot-id)
    (catch AmazonServiceException e
      (if (= (.getErrorCode e) "InvalidSnapshot.NotFound")
        (printf "%s doesn't exist, skipping \n" snapshot-id)
        (printf "ERROR deleting %s: %s\n" snapshot-id e)))))

(defn safe-delete-snapshot
  "delete a snapshot only if it belongs to an image that does not exist"
  [{:keys [snapshot-id image-id]}]
  (if image-id
    (try
      (if (-> (ec2/describe-images :image-ids [image-id])
              :images
              seq)
        (printf "skipping %s because %s still exists\n" snapshot-id image-id)
        (delete-snapshot snapshot-id))
      (catch AmazonServiceException e
        (if (= (.getErrorCode e) "InvalidAMIID.NotFound")
          (delete-snapshot snapshot-id)
          (printf "skipping %s because describe returned an unexpected error: %s\n" snapshot-id e))))
    (printf "skipping %s because it does nopt belong to an image\n" snapshot-id)))

(defn purge-orphan-snapshots
  "Find and delete snapshots that once belonged to an AMI, but the AMI no longer exists"
  [owner-ids]
  (let [orphans (find-orphaned-snapshots owner-ids)]
    (printf "will delete %d orphaned snapshots\n" (count orphans))
    (doseq [s orphans]
      (safe-delete-snapshot s))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
