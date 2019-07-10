Extension { #name : #SpProgressBarProgressing }

{ #category : #'*Spec2-Adapters-Morphic' }
SpProgressBarProgressing >> customizeMorphicBar: aProgressBarMorph [
	self flag: #todo.	"This is not the best solution but we do not know yet what is. Pablo would like to have a minimal version of TaskIt in the image that could be used. But at least this is only in one place and we can update it easily."
	[ [ self currentValue < 1 ]
		whileTrue: [ self currentValue: self progression value.
			aProgressBarMorph value: self currentValue.
			self refreshDelay wait ] ] fork
]
