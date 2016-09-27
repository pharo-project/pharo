instance creation
showSourceWithConverter: aConverter
	| behavior |
	
	behavior := browser textModel behavior.
	browser textModel: browser newText.
	browser textModel beForCode.
	browser needRebuild: false.
	browser textModel behavior: behavior.
	browser textConverter: aConverter.

	browser openWithSpec.
