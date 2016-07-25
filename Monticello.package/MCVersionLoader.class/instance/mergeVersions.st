loading
mergeVersions
	|merger|
	merger :=  MCVersionMerger new.
	merger addVersions: self localModifications.
	merger merge.