Class {
	#name : #WebResolver,
	#superclass : #PlatformResolver,
	#category : #'FileSystem-Core-Base-Resolver'
}

{ #category : #accessing }
WebResolver class >> platformName [
	^ 'Web'
]

{ #category : #origins }
WebResolver >> cache [
	"Operating Systems often define standard locations for a personal cache directory. The cache directory is a user-specific non-essential (cached) place where data should be written."
	^'/SqueakJS'
]

{ #category : #origins }
WebResolver >> desktop [
	^'/SqueakJS'
]

{ #category : #origins }
WebResolver >> documents [
	^'/SqueakJS'
]

{ #category : #origins }
WebResolver >> home [
	^'/SqueakJS'
]

{ #category : #origins }
WebResolver >> preferences [
	^'/SqueakJS'
]

{ #category : #origins }
WebResolver >> temp [
	"Where to put files that are not supposed to last long"
	^'/SqueakJS'
]
