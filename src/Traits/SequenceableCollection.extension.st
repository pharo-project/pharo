Extension { #name : #SequenceableCollection }

{ #category : #'*Traits' }
SequenceableCollection >> asTraitComposition [
	"For convenience the composition {T1. T2 ...} is the same as T1 + T2 + ..."
	^self isEmpty
		ifFalse: [
			self size = 1
				ifTrue: [self first asTraitComposition]
				ifFalse: [
					self copyWithoutFirst 
						inject: self first
						into: [:left :right | left + right]]]
		ifTrue: [
			TraitComposition new]
]
