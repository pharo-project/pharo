"
I am a visitor for EpCodeChange subinstances.
"
Trait {
	#name : 'EpTCodeChangeVisitor',
	#category : 'Epicea-Visitors',
	#package : 'Epicea',
	#tag : 'Visitors'
}

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitBehaviorChange: aChange [
	^ self visitCodeChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitBehaviorCommentChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitBehaviorNameChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [
	^ self visitBehaviorChange: aBehaviorRepackagedChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitClassAddition: aChange [
	^ self visitClassChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitClassChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitClassModification: aChange [
	^ self visitClassChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitClassRemoval: aChange [
	^ self visitClassChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitCodeChange: aChange [
	^ self explicitRequirement
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitMethodAddition: aChange [
	^ self visitMethodChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitMethodChange: aChange [
	^ self visitCodeChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitMethodModification: aChange [
	^ self visitMethodChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitMethodRemoval: aChange [
	^ self visitMethodChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitPackageAddition: aPackageAddition [

	^ self visitPackageChange: aPackageAddition
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitPackageChange: aChange [
	^ self visitCodeChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitPackageRemoval: aPackageRemoval [

	^ self visitPackageChange: aPackageRemoval
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitPackageRename: aPackageRename [

	^ self visitPackageChange: aPackageRename
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitPackageTagAddition: aPackageTagAddition [
	^ self visitPackageTagChange: aPackageTagAddition
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitPackageTagChange: aChange [

	^ self visitCodeChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitPackageTagRemoval: aPackageTagRemoval [
	^ self visitPackageTagChange: aPackageTagRemoval
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitPackageTagRename: aPackageTagRename [
	^ self visitPackageTagChange: aPackageTagRename
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitProtocolAddition: aProtocolAddition [
	^ self visitProtocolChange: aProtocolAddition
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitProtocolChange: aProtocolAddition [
	^ self visitBehaviorChange: aProtocolAddition
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitProtocolRemoval: aProtocolRemoved [
	^ self visitProtocolChange: aProtocolRemoved
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitTraitAddition: aChange [
	^ self visitTraitChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitTraitChange: aChange [
	^ self visitBehaviorChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitTraitModification: aChange [
	^ self visitTraitChange: aChange
]

{ #category : 'visitor' }
EpTCodeChangeVisitor >> visitTraitRemoval: aChange [
	^ self visitTraitChange: aChange
]
