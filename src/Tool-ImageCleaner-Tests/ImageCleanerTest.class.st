"
An ImageCleanerTest is a test class for testing the behavior of ImageCleaner
"
Class {
	#name : #ImageCleanerTest,
	#superclass : #TestCase,
	#category : #'Tool-ImageCleaner-Tests'
}

{ #category : #accessing }
ImageCleanerTest >> imageCleanerClass [ 
	"returns the class that represents image cleaner"
	^ ImageCleaner
]

{ #category : #test }
ImageCleanerTest >> testTestPackages [

	"ImageCleaner testPackages method returns just symbols. Maybe it should be renamed."

	self
		assert: (self imageCleanerClass testPackages allSatisfy: #isSymbol)
		equals: true.

	"All test package names are candidates for cleanup, except ReleaseTests package"
	self
		assert: (self imageCleanerClass testPackages noneSatisfy: [ :aSymbol | 
				 aSymbol = #'ReleaseTests' ])
		equals: true
]
