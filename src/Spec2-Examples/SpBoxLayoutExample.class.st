Class {
	#name : #SpBoxLayoutExample,
	#superclass : #SpPresenter,
	#traits : 'SpTDynamicPresenter',
	#classTraits : 'SpTDynamicPresenter classTrait',
	#instVars : [
		'element11',
		'element21',
		'element12',
		'element22',
		'properties'
	],
	#category : #'Spec2-Examples-Layouts'
}

{ #category : #'instance creation' }
SpBoxLayoutExample class >> open [ 
	<script>

	self new openWithSpec
]

{ #category : #adding }
SpBoxLayoutExample >> add: anObject properties: symbol to: aLayout [
	aLayout
		add: anObject
		withConstraints: [ :constraints | 
			constraints
				expand: ((properties at: symbol) at: #expanded);
				fill: ((properties at: symbol) at: #filled);
				padding: ((properties at: symbol) at: #padding) ]
]

{ #category : #adding }
SpBoxLayoutExample >> add: symbol to: aLayout [
	self add: symbol properties: symbol to: aLayout
]

{ #category : #initialization }
SpBoxLayoutExample >> initialize [
	properties := Dictionary new.
	super initialize
]

{ #category : #initialization }
SpBoxLayoutExample >> initializePresenters [
	element11 := self newButton label: #element11.
	element21 := self newButton label: #element21.
	element12 := self newButton label: #element12.
	element22 := self newButton label: #element22.

	#('element11' 'element12' 'element21' 'element22' 'Row1' 'Row2')
		do: [ :ele | 
			properties at: ele put: (Dictionary with: #expanded -> true with: #filled -> true with: #padding -> 5).
			self
				presenterAt: ele , 'Expanded'
				put:
					(self newCheckBox
						label: 'Expanded';
						state: true;
						whenChangedDo: [ :value | 
							(properties at: ele) at: #expanded put: value.
							self refresh ];
						yourself).
			self
				presenterAt: ele , 'Filled'
				put:
					(self newCheckBox
						label: 'Filled';
						state: true;
						whenChangedDo: [ :value | 
							(properties at: ele) at: #filled put: value.
							self refresh ];
						yourself).
			self
				presenterAt: ele , 'Padding'
				put:
					(self newSlider
						min: 0;
						max: 50;
						value: 5;
						whenValueChangedDo: [ :value | 
							(properties at: ele) at: #padding put: value.
							self refresh ];
						yourself) ]
]

{ #category : #initialization }
SpBoxLayoutExample >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: 'Box layout example';
		initialExtent: 600 @ 800
]

{ #category : #accessing }
SpBoxLayoutExample >> layout [
	| main content subLayout subLayout2 options |
	main := SpBoxLayout newVertical.

	subLayout := SpBoxLayout newHorizontal.
	self add: #element11 to: subLayout.
	self add: #element12 to: subLayout.

	subLayout2 := SpBoxLayout newHorizontal.
	self add: #element21 to: subLayout2.
	self add: #element22 to: subLayout2.

	content := SpBoxLayout newVertical.

	self add: subLayout properties: 'Row1' to: content.
	self add: subLayout2 properties: 'Row2' to: content.

	main add: content.

	self flag: #todo. "Replace the code by the commented on when https://github.com/pharo-spec/Spec/issues/749 will be fixed."
	"options := SpGridLayout new.
	#('Row1' 'Row2' 'element11' 'element12' 'element21' 'element22')
		doWithIndex: [ :ele :ind | 
			options
				add:
					(SpBoxLayout newVertical
						add: ele capitalized;
						add:
							(SpBoxLayout newHorizontal
								add: (self presenterAt: ele , 'Expanded');
								add: (self presenterAt: ele , 'Filled');
								yourself);
						add:
							(SpBoxLayout newHorizontal
								add: 'Padding';
								add: (self presenterAt: ele , 'Padding');
								yourself);
						yourself)
				at: ((ind + 1) \\ 2 + 1) @ ((ind + 1) / 2) floor ]."

	options := SpBoxLayout newVertical.
	#('Row1' 'Row2' 'element11' 'element12' 'element21' 'element22')
		doWithIndex: [ :ele :ind | 
			options
				add:
					(SpBoxLayout newVertical
						add: ele capitalized;
						add:
							(SpBoxLayout newHorizontal
								add: (self presenterAt: ele , 'Expanded');
								add: (self presenterAt: ele , 'Filled');
								yourself);
						add:
							(SpBoxLayout newHorizontal
								add: 'Padding';
								add: (self presenterAt: ele , 'Padding');
								yourself);
						yourself) ].
	main add: options.

	^ main
]

{ #category : #'private accessing' }
SpBoxLayoutExample >> refresh [
	self needRebuild: false.
	self buildWithSpec
]
