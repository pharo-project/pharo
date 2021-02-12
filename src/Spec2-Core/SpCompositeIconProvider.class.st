"
A composite icon provider.
Users can compose icon providers to define different ways to find icons.
A typical usage would be: 

```
(SpLocationIconProvider newLocation: './icons' asFileReference), SpPharoThemeIconProvider new
```
"
Class {
	#name : #SpCompositeIconProvider,
	#superclass : #SpIconProvider,
	#instVars : [
		'providers'
	],
	#category : #'Spec2-Core-IconProvider'
}

{ #category : #copying }
SpCompositeIconProvider >> , aProvider [

	self addProvider: aProvider.
	^ self
]

{ #category : #accessing }
SpCompositeIconProvider >> addProvider: anIconProvider [

	providers := providers copyWith: anIconProvider
]

{ #category : #accessing }
SpCompositeIconProvider >> iconNamed: aName ifAbsent: aBlock [

	self providers do: [ :each |
		(each iconNamed: aName ifAbsent: [ nil ]) 
			ifNotNil: [ :icon | ^ icon ] ].
	
	^ aBlock value
]

{ #category : #initialization }
SpCompositeIconProvider >> initialize [

	super initialize.
	providers := #()
]

{ #category : #accessing }
SpCompositeIconProvider >> providers [
	
	^ providers
]
