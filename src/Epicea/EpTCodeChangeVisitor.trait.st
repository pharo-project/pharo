"
I am a visitor for EpCodeChange subinstances.
"
Trait {
	#name : #EpTCodeChangeVisitor,
	#category : 'Epicea-Visitors'
}

{ #category : #visitor }
EpTCodeChangeVisitor >> visitBehaviorCategoryChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitBehaviorChange: aChange [
	^ self visitCodeChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitBehaviorCommentChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitBehaviorNameChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitCategoryAddition: aChange [
	^ self visitCategoryChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitCategoryChange: aChange [
	^ self visitCodeChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitCategoryRemoval: aChange [
	^ self visitCategoryChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitCategoryRename: aChange [
	^ self visitCategoryChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitClassAddition: aChange [
	^ self visitClassChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitClassChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitClassModification: aChange [
	^ self visitClassChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitClassRemoval: aChange [
	^ self visitClassChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitCodeChange: aChange [
	^ self explicitRequirement
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitMethodAddition: aChange [
	^ self visitMethodChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitMethodChange: aChange [
	^ self visitCodeChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitMethodModification: aChange [
	^ self visitMethodChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitMethodRemoval: aChange [
	^ self visitMethodChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitProtocolAddition: aProtocolAddition [
	^ self visitProtocolChange: aProtocolAddition
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitProtocolChange: aProtocolAddition [
	^ self visitBehaviorChange: aProtocolAddition.
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitProtocolRemoval: aProtocolRemoved [
	^ self visitProtocolChange: aProtocolRemoved
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitTraitAddition: aChange [
	^ self visitTraitChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitTraitChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitTraitModification: aChange [
	^ self visitTraitChange: aChange
]

{ #category : #visitor }
EpTCodeChangeVisitor >> visitTraitRemoval: aChange [
	^ self visitTraitChange: aChange
]
