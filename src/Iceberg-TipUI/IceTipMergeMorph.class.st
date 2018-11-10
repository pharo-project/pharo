"
I redefine PSMCMergeMorph to be able to use an IceMergePatchMorph as patch morph
"
Class {
	#name : #IceTipMergeMorph,
	#superclass : #PSMCMergeMorph,
	#category : #'Iceberg-TipUI-Merge'
}

{ #category : #accessing }
IceTipMergeMorph >> conflictCount [
	"Answer the number of conflicts that are unresolved."

	^(self model ifNil: [^0]) unresolvedConflictCount
]

{ #category : #initialization }
IceTipMergeMorph >> model: aMerger [
	"Set the model and update the window."

	"Inlined/Copied from MorphicModel because we need to overwrite superclass' behavior"
	model ifNotNil: [aMerger removeDependent: self].
	aMerger ifNotNil: [aMerger addDependent: self].
	model := aMerger.
	
	self patchMorph model: aMerger
]

{ #category : #'instance creation' }
IceTipMergeMorph >> newPatchMorph [
	"Answer a new patch morph."

	^IceTipMergePatchMorph new
		borderWidth: 0;
		addDependent: self;
		yourself
]
