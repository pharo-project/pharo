Extension { #name : #ClyNavigationEnvironment }

{ #category : #'*Calypso-SystemQueries' }
ClyNavigationEnvironment class >> currentImage [
	^self defaultOver: ClySystemEnvironment currentImage
]

{ #category : #'*Calypso-SystemQueries' }
ClyNavigationEnvironment class >> currentImageScope [
	^self currentImage systemScope
]

{ #category : #'*Calypso-SystemQueries' }
ClyNavigationEnvironment class >> overSystemEnvironment: aSystemEnvironment [
	^(self over: aSystemEnvironment)
		addPlugin: ClyDefaultSystemEnvironmentPlugin new
]

{ #category : #'*Calypso-SystemQueries' }
ClyNavigationEnvironment class >> queryCurrentImageFor: aQuery [
	^self currentImageScope query: aQuery
]
