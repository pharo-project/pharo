instance creation
showDiffMorphWithConverter: aConverter
	| behavior |
	
	behavior := browser textModel behavior.
	browser textModel: browser newDiff.
	browser needRebuild: false.
	browser openWithSpec.
	browser textConverter: aConverter.
	browser textModel behavior: behavior