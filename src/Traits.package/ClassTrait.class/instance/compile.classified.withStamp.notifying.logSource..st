compiling
compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	
	<localClassTraitMethod> 
    "This method is supposed to be local in ClassTrait because of a good reason.
    We use this pragma to test if ClassTrait does not contain some accidental 
    local selectors."

	| classSideUsersOfBaseTrait message |
	
	classSideUsersOfBaseTrait := self baseTrait traitUsers select: [:each | each isClassSide].
	
	classSideUsersOfBaseTrait isEmpty ifFalse: [
		message := String streamContents: [ :stream |
			stream nextPutAll: 'The instance side of this trait is used on '; cr.
			classSideUsersOfBaseTrait
				do: [:each | stream nextPutAll: each name ]
				separatedBy: [ stream nextPutAll: ', ' ].
			stream cr; nextPutAll: ' You can not add methods to the class side of this trait!'].
		^ TraitException signal:  message ].
	
	^ super
		compile: text
		classified: category
		withStamp: changeStamp
		notifying: requestor
		logSource: logSource