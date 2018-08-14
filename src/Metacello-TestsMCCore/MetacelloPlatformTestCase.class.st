Class {
	#name : #MetacelloPlatformTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsMCCore'
}

{ #category : #tests }
MetacelloPlatformTestCase >> testTempFileFor [
	"This should be enough to ensure that https://github.com/dalehenrich/metacello-work/issues/405 doesn't happen again"
	| tempFilePath |

	tempFilePath := MetacelloPlatform current tempFileFor: 'xxx' suffix: '.test'.

]
