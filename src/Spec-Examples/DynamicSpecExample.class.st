"
I am an example of how dynamically create the UI corresponding at an object.

self exampleInt
self exampleNil
self exampleString
"
Class {
	#name : #DynamicSpecExample,
	#superclass : #Object,
	#instVars : [
		'ui',
		'object'
	],
	#category : 'Spec-Examples-Wrapper'
}

{ #category : #example }
DynamicSpecExample class >> exampleInt [
	^ self new open: 42
]

{ #category : #example }
DynamicSpecExample class >> exampleNil [
	^ self new
		open: nil
]

{ #category : #example }
DynamicSpecExample class >> exampleString [
	^ self new
		open: 'Hello World'
]

{ #category : #'instance creation' }
DynamicSpecExample class >> object: object [

	^ self new
		object: object;
		yourself
]

{ #category : #initialization }
DynamicSpecExample >> initialize [

	super initialize.
	object := nil asValueHolder.
]

{ #category : #accessing }
DynamicSpecExample >> object [

	^ object value
]

{ #category : #accessing }
DynamicSpecExample >> object: anObject [
	
	object value: anObject
]

{ #category : #protocol }
DynamicSpecExample >> open [

	object value ifNil: [ ^ self openOnNil ].
	object value isInteger ifTrue: [ ^ self openOnInteger ].
	object value isString ifTrue: [ ^ self openOnString ]
]

{ #category : #protocol }
DynamicSpecExample >> open: anObject [
	anObject ifNil: [ ^ self openOnNil ].
	anObject isInteger
		ifTrue: [ ^ self openOnInteger: anObject ].
	anObject isString
		ifTrue: [ ^ self openOnString: anObject ]
]

{ #category : #protocol }
DynamicSpecExample >> openOnInteger [
	<script: '(DynamicSpecExample object: 42) open'>
	
	| anUI layout |
	
	anUI := DynamicComposablePresenter new.
	anUI title: self title.
	anUI instantiatePresenters: #( 
		text LabelPresenter
		plus ButtonPresenter
		minus ButtonPresenter ).
		
	anUI text label: object value asString.
	anUI minus 
		label: '-';
		state: false;
		action: 	[ 
			object value: object value -1.
			anUI text label: object value asString ].

	anUI plus 
		label: '+';
		state: false;
		action: 	[ 
			object value: object value +1.
			anUI text label: object value asString ].
	
	layout := SpecLayout composed
		newColumn: [ :c |
			c
				add: #text height: 25;
				newRow: [ :r | r add: #minus ; addSplitter; add: #plus ] height: 25 ];
		yourself.
				
	anUI openWithSpecLayout: layout.
]

{ #category : #protocol }
DynamicSpecExample >> openOnInteger: anInteger [
	ui := OpenOnIntExample new: anInteger.
	ui
		extent: 100 @ 100;
		openWithSpec
]

{ #category : #protocol }
DynamicSpecExample >> openOnNil [
	ui := OpenOnNilExample new.
	ui 
		extent: 200 @ 100; openWithSpec
]

{ #category : #protocol }
DynamicSpecExample >> openOnString [
	<script: '(DynamicSpecExample object: ''Pharo'') open'>
	
	| anUI layout bottomLayout temp |	
	anUI := DynamicComposablePresenter new.
	anUI title: self title.
	anUI instantiatePresenters: #( 
		label LabelPresenter
		check CheckBoxPresenter
		text TextInputFieldPresenter
		button ButtonPresenter ).
		
	anUI label label: object value.

			
	object whenChangedDo: [ :o | anUI label label: o asString ].
		
	anUI text 
		text: object value;
		enabled: true;
		acceptBlock: [ :string | self object: string ].
	
	temp := object value.
	
	anUI button
		label: 'reset';
		state: false;
		action: [ 
			anUI text text: temp.
			self object: temp ].
		
	layout := SpecLayout composed
				newColumn: [ :c |
					c 
						newRow: [ :r | r add: #label; add: #check width: 100 ] height: 25;
						newRow: [ :r | r add: #text; add: #button ] height: 25 ];
				yourself.
				
	bottomLayout := SpecLayout composed
				newColumn: [ :c |
					c 
						newRow: [ :r | r add: #text; add: #button ] height: 25;
						newRow: [ :r | r add: #label; add: #check width: 100 ] height: 25 ];
				yourself.
	
	anUI check
		label: 'Label on top';
		state: true;
		whenActivatedDo: [ 
			anUI needFullRebuild: false.
			anUI buildWithSpecLayout: layout ];
		whenDeactivatedDo: [ 
			anUI needFullRebuild: false.
			anUI buildWithSpecLayout: bottomLayout ].
				
	anUI openWithSpecLayout: layout.
]

{ #category : #protocol }
DynamicSpecExample >> openOnString: aString [
	ui := OpenOnStringExample new:aString.
	ui 
		extent: 300 @ 200;openWithSpec
]

{ #category : #accessing }
DynamicSpecExample >> title [

	^ 'Dynamic Examples'
]
