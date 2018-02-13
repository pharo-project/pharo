"
Utility class to check packages
"
Class {
	#name : #PackageChecker,
	#superclass : #Object,
	#category : #Deprecated70
}

{ #category : #check }
PackageChecker >> check [
	"self new check"
	
	"only work for top level package overlapping.
	Foo and Foo-Core, but not Foo-Core and Foo-Core-Test"
	| packageNames dict overlappingPackages |
	overlappingPackages := OrderedCollection new.
	packageNames := MCWorkingCopy allManagers collect: [:each | each packageName].
	dict := packageNames groupedBy: [:each | each copyUpTo: $-].
	dict copy keysAndValuesDo: [:k :v | v size = 1 ifTrue: [ dict removeKey: k]].
	dict keysAndValuesDo: [:k :v | (v includes: k) 
									ifTrue: [
										overlappingPackages add: v.
										Warning signal: 'Overlapping packages: ', v printString]].
	^ overlappingPackages 
]
