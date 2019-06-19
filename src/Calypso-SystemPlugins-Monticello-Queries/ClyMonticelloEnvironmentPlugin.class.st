"
I mark dirty packages with special tag
"
Class {
	#name : #ClyMonticelloEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-Monticello-Queries'
}

{ #category : #controlling }
ClyMonticelloEnvironmentPlugin >> attachToSystem [

	environment system subscribe: self for: MCPackageModified
]

{ #category : #'item decoration' }
ClyMonticelloEnvironmentPlugin >> decorateBrowserItem: anItem ofPackage: aPackage [

	| workingCopy |

	workingCopy := aPackage mcWorkingCopy.
	workingCopy ifNil: [ ^self ].
	
	workingCopy modified ifTrue: [
		anItem markWith: ClyDirtyPackageTag].
]

{ #category : #controlling }
ClyMonticelloEnvironmentPlugin >> detachFromSystem [

	environment system unsubscribe: self
]

{ #category : #controlling }
ClyMonticelloEnvironmentPlugin >> systemChanged: aPackageModified [

	environment systemChanged: aPackageModified
]
