"
I am an abstract adapter for tallies (from different system profilers) to be used by the ProfilerPresenter
"
Class {
	#name : 'TallyModel',
	#superclass : 'MorphTreeNodeModel',
	#category : 'ProfilerUI-Model',
	#package : 'ProfilerUI',
	#tag : 'Model'
}

{ #category : 'comparing' }
TallyModel >> <= aTallyModelAndreas [
	^ self item tally <= aTallyModelAndreas item tally
]

{ #category : 'comparing' }
TallyModel >> = aTallyModel [

	^ self item tally = aTallyModel item tally and: [
		  self item class = aTallyModel item class and: [
			self item method = aTallyModel item method ] ]
]

{ #category : 'converting' }
TallyModel >> asString [

	^ [ self reportString ]
		  on: Error
		  do: [ 'error printing' ]
]

{ #category : 'transforming' }
TallyModel >> asTallyModel [
	^ self
]

{ #category : 'actions' }
TallyModel >> browseItem [
	self item originMethod browse
]

{ #category : 'accessing' }
TallyModel >> childrenItems [
	| perCent threshold |

	model tallyRoot ifNil: [ ^#() ].

	perCent := model showNodeThreshold.
	threshold := (perCent asFloat / 100 * model tallyRoot tally) rounded.

	^ (self item sonsOver: threshold) asSortedCollection reverse
]

{ #category : 'accessing' }
TallyModel >> data [
	^ self
]

{ #category : 'actions' }
TallyModel >> inspectItem [
	self inspect
]

{ #category : 'event handling' }
TallyModel >> keyDown: anEvent from: aTreeMorph [
	| c |
	c := anEvent keyCharacter.
	c = $b
		ifTrue: [self browseItem. ^ true].
	c = $i
		ifTrue: [self inspectItem. ^ true].
	^ false
]

{ #category : 'collecting leaves' }
TallyModel >> leavesInto: aCollection fromSender: anObject [

	item leavesInto: aCollection fromSender: anObject.
	aCollection associationsDo: [ :assoc |
		assoc value: (assoc value asTallyModel
				 model: model;
				 yourself) ]
]

{ #category : 'menu' }
TallyModel >> menu: menu shifted: b [
	(menu
		add: 'Browse (b)'
		target: self
		selector: #browseItem) icon: (self iconNamed: #glamorousBrowse).
	(menu
		add: 'Inspect (i)'
		target: self
		selector: #inspectItem) icon: (self iconNamed: #glamorousInspect)
]

{ #category : 'accessing' }
TallyModel >> methodName [

	^ item methodClassName , '>>' , (item method
		   ifNotNil: [ item method selector ]
		   ifNil: [ 'nil' ])
]

{ #category : 'accessing' }
TallyModel >> methodOrBlock [
	item ifNil: [ ^ nil ].
	^ item method
]

{ #category : 'path' }
TallyModel >> relativePath [

	| rootPath |
	rootPath := model tallyRoot path.

	rootPath doWithIndex: [ :each :index |
		self assert: each = (self path at: index) ].
	^ self path copyFrom: rootPath size to: self path size
]

{ #category : 'querying' }
TallyModel >> reportString [

	^ (self weightToRoot printShowingDecimalPlaces: 1) , '% {'
	  , (self time printShowingDecimalPlaces: 1) , 'ms} ' , self methodName
]

{ #category : 'querying' }
TallyModel >> sons [

	^ self sonsOver: 0
]

{ #category : 'accessing' }
TallyModel >> sonsOver: minTally [
	^ (item sonsOver: minTally) collect: [ :each |
		  each asTallyModel
			  model: model;
			  yourself ]
]

{ #category : 'accessing' }
TallyModel >> tally [
	^ item tally
]

{ #category : 'accessing' }
TallyModel >> time [
	^ (self weight * self model totalTime / 100)
]

{ #category : 'accessing' }
TallyModel >> weight [
	self tally isZero ifTrue: [ ^ 0 ].
	^ self tally asFloat / model totalTally * 100.0
]

{ #category : 'accessing' }
TallyModel >> weightToRoot [
	self tally isZero ifTrue: [ ^ 0 ].
	model tallyRoot tally isZero ifTrue: [ self error: 'this shouldnt happen' ].
	^ self tally asFloat / model tallyRoot tally * 100.0
]
