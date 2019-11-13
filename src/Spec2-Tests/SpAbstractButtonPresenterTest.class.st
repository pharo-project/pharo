Class {
	#name : #SpAbstractButtonPresenterTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #testing }
SpAbstractButtonPresenterTest class >> isAbstract [

	^ super isAbstract or: [ self = SpAbstractButtonPresenterTest ]
]

{ #category : #tests }
SpAbstractButtonPresenterTest >> testWhenHelpChanged [
	presenter help: 'label1'.
	presenter
		whenHelpChangedDo: [ :new :old | 
			self assert: old equals: 'label1'.
			self assert: new equals: 'label2' ].
	self assert: presenter help equals: 'label1'.
	self openInstance.
	presenter help: 'label2'.
	self assert: presenter help equals: 'label2'
]

{ #category : #tests }
SpAbstractButtonPresenterTest >> testWhenIconChangedDo [

	presenter iconName: #glamorousCancel.

	presenter whenLabelChangedDo: [ :new :old | 
		self assert: new equals: (self iconNamed: #glamorousCancel)].

	self assert: presenter icon equals: (self iconNamed: #glamorousCancel).

	self openInstance.

	presenter iconName: #pharo.

	self assert: presenter icon equals: (self iconNamed: #pharo).

]

{ #category : #tests }
SpAbstractButtonPresenterTest >> testWhenLabelChangedDo [

	presenter label: 'label1'.

	presenter whenLabelChangedDo: [ :new :old | 
		self assert: old equals: 'label1'. self assert: new equals: 'label2'].

	self assert: presenter label equals: 'label1'.

	self openInstance.
	presenter label: 'label2'.

	self assert: presenter label equals: 'label2'.

]
