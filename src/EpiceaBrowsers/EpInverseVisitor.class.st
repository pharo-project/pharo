"
I convert a code change to it's inverse operation. For example, a class addition into a class removal.
"
Class {
	#name : #EpInverseVisitor,
	#superclass : #EpLogBrowserOperationVisitor,
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#category : #'EpiceaBrowsers-Visitors'
}

{ #category : #visitor }
EpInverseVisitor >> visitBehaviorCategoryChange: aChange [

	^ EpBehaviorCategoryChange
			oldCategory: aChange newCategory
			newCategory: aChange oldCategory
			class: aChange behaviorAffected

]

{ #category : #visitor }
EpInverseVisitor >> visitBehaviorCommentChange: aChange [

	^ EpBehaviorCommentChange
			newWithBehavior: aChange behaviorAffected
			oldComment: aChange newComment
			newComment: aChange oldComment
			oldStamp: aChange newStamp
			newStamp: aChange oldStamp

]

{ #category : #visitor }
EpInverseVisitor >> visitBehaviorNameChange: aChange [

	^ EpBehaviorNameChange
			oldName: aChange newName
			newName: aChange oldName
			class: aChange behaviorAffected
]

{ #category : #visitor }
EpInverseVisitor >> visitCategoryAddition: aChange [

	^ EpCategoryRemoval 
			named: aChange categoryName
			packageName: aChange affectedPackageName
]

{ #category : #visitor }
EpInverseVisitor >> visitCategoryRemoval: aChange [
	
	^ EpCategoryAddition
			named: aChange categoryName
			packageName: aChange affectedPackageName
]

{ #category : #visitor }
EpInverseVisitor >> visitCategoryRename: aChange [

	^ EpCategoryRename
			oldName: aChange newCategoryName
			newName: aChange oldCategoryName
			packageName: aChange affectedPackageName

]

{ #category : #visitor }
EpInverseVisitor >> visitClassAddition: aChange [

	^ EpClassRemoval class: aChange behaviorAffected
]

{ #category : #visitor }
EpInverseVisitor >> visitClassModification: aChange [

	^ EpClassModification oldClass: aChange newClass newClass: aChange oldClass.

]

{ #category : #visitor }
EpInverseVisitor >> visitClassRemoval: aChange [

	^ EpClassAddition class: aChange behaviorAffected
]

{ #category : #visitor }
EpInverseVisitor >> visitCodeChange: aChange [
	^ self error: 'Should not be reached'
]

{ #category : #visitor }
EpInverseVisitor >> visitMethodAddition: aChange [

	^ EpMethodRemoval method: aChange methodAffected
]

{ #category : #visitor }
EpInverseVisitor >> visitMethodModification: aChange [

	^ EpMethodModification oldMethod: aChange newMethod newMethod: aChange oldMethod
]

{ #category : #visitor }
EpInverseVisitor >> visitMethodRemoval: aChange [

	^ EpMethodAddition method: aChange methodAffected
]

{ #category : #visitor }
EpInverseVisitor >> visitProtocolAddition: aChange [

	^ EpProtocolRemoval behavior: aChange behaviorAffected protocol: aChange protocol
]

{ #category : #visitor }
EpInverseVisitor >> visitProtocolRemoval: aChange [

	^ EpProtocolAddition behavior: aChange behaviorAffected protocol: aChange protocol

]

{ #category : #visitor }
EpInverseVisitor >> visitTraitAddition: aChange [
	
	^ EpTraitRemoval trait: aChange behaviorAffected
]

{ #category : #visitor }
EpInverseVisitor >> visitTraitModification: aTraitModification [

	self notYetImplemented
]

{ #category : #visitor }
EpInverseVisitor >> visitTraitRemoval: aChange [

	^ EpTraitAddition trait: aChange behaviorAffected
]
