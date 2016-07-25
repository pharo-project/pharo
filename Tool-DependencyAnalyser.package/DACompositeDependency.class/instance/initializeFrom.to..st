initialization
initializeFrom: aSourcePackage to: aTargetPackage

	super initializeFrom: aSourcePackage to: aTargetPackage.
	dependencies := OrderedCollection new