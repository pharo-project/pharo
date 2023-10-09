"
I represent a change in the package or package tag of a class or a trait.
"
Class {
	#name : 'EpBehaviorRepackagedChange',
	#superclass : 'EpBehaviorChange',
	#instVars : [
		'oldTagName',
		'oldPackageName',
		'newTagName',
		'newPackageName',
		'class'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'instance creation' }
EpBehaviorRepackagedChange class >> oldPackage: oldPackage oldTag: tagName newPackage: newPackage newTag: newTagName class: aClass [

	^ self new
		  oldPackageName: oldPackage;
		  oldTagName: tagName;
		  newPackageName: newPackage;
		  newTagName: newTagName;
		  classAffected: aClass;
		  yourself
]

{ #category : 'visitor' }
EpBehaviorRepackagedChange >> accept: aVisitor [

	^ aVisitor visitBehaviorRepackagedChange: self
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> behaviorAffected [
	^ self classAffected
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> behaviorAffectedName [
	^ self classAffected name
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> classAffected [

	^ class
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> classAffected: aBehavior [

	class := aBehavior asEpiceaRingDefinition
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> newPackageName [

	^ newPackageName
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> newPackageName: anObject [

	newPackageName := anObject
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> newTagName [

	^ newTagName
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> newTagName: anObject [

	newTagName := anObject
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> oldPackageName [

	^ oldPackageName
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> oldPackageName: anObject [

	oldPackageName := anObject
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> oldTagName [

	^ oldTagName
]

{ #category : 'accessing' }
EpBehaviorRepackagedChange >> oldTagName: anObject [

	oldTagName := anObject
]
