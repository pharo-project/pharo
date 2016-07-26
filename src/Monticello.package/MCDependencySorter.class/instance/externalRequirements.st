accessing
externalRequirements
	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (unloaded flatCollect: [:e | e provisions]) asSet.
	^ required keys reject: [:ea | providedByUnloaded includes: ea ]