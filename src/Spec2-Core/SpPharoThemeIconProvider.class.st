"
This icon provider redirects to the default Pharo icon provider
"
Class {
	#name : #SpPharoThemeIconProvider,
	#superclass : #SpIconProvider,
	#category : #'Spec2-Core-IconProvider'
}

{ #category : #accessing }
SpPharoThemeIconProvider >> iconNamed: aName ifAbsent: aBlock [

	^ ThemeIcons current 
		iconNamed: aName 
		ifNone: aBlock
]
