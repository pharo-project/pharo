"
I transform libgit errors into Iceberg errors using a double dispatch.
In case there is no case implemented for an exception a generic error will be raised instead.

Each case in the double dispatch implements transformations for a specific kind of libgit error such as ENOTFOUND, ECONFLICT and so on. However, each libgit error can be mapped to several high-level Iceberg exceptions. It is the responsibility of this error visitor to do the matching of the different information provided by a libgit error (error code, description, message text) and decide which exception to use.

I also have a context, which points to the object that is requesting the exception transformation.
This context is used to obtain more information about the operation that failed, usually not provided by Libgit, such as urls, locations, etc.
"
Class {
	#name : #IceLibgitErrorVisitor,
	#superclass : #Object,
	#instVars : [
		'context'
	],
	#category : #'Iceberg-Libgit-Exceptions'
}

{ #category : #'instance creation' }
IceLibgitErrorVisitor class >> onContext: aContext [

	^ self new
		context: aContext;
		yourself
]

{ #category : #accessing }
IceLibgitErrorVisitor >> context [
	^ context
]

{ #category : #accessing }
IceLibgitErrorVisitor >> context: anObject [
	context := anObject
]

{ #category : #visiting }
IceLibgitErrorVisitor >> visitECONFLICT: anError [
	anError object =  LGitErrorTypeEnum giterr_checkout
		ifTrue: [ ^ anError resignalAs: IceCheckoutConflictError new ].
	self visitGenericError: anError
]

{ #category : #visiting }
IceLibgitErrorVisitor >> visitEEOF: aLGit_GIT_EEOF [

	aLGit_GIT_EEOF messageText trimmed = 'ERROR: Repository not found.'
		ifTrue: [ IceCloneRemoteNotFound signalFor: context url ].
	
	^ self visitGenericError: aLGit_GIT_EEOF
]

{ #category : #visiting }
IceLibgitErrorVisitor >> visitEEXISTS: anError [

	(anError messageText beginsWith: 'Failed to write reference ''refs/tags')
		ifTrue: [ | refName messageStream|
			messageStream := anError messageText readStream.
			refName := messageStream
				upTo: $/;
				upTo: $/;
				upTo: $'.
			IceTagAlreadyExists new
				tagName: refName;
				signal ].
	self visitGenericError: anError
]

{ #category : #visiting }
IceLibgitErrorVisitor >> visitENOTFOUND: anError [

	NotFound signal: anError messageText
]

{ #category : #visiting }
IceLibgitErrorVisitor >> visitERROR: anError [

	(anError object = LGitErrorTypeEnum giterr_ssh)
		ifTrue: [ anError resignalAs: IceAuthenticationError new ].
	"user errors needs to be handled differently, since they do not have an 
	 actual code. This is a HACK, but it will fix #577"
	(anError object value = LGitReturnCodeEnum git_euser value)
		ifTrue: [ self visitEUSER: anError  ].
	self visitGenericError: anError
]

{ #category : #visiting }
IceLibgitErrorVisitor >> visitEUSER: anError [
	"Since user errors do not have a code, they can can just be handled by analysis 
	 of messageText. String comparisson is not the thing in history, but for not it will 
	 fit. :("
	anError messageText = 'No ssh-agent suitable credentials found.'
		ifTrue: [ anError resignalAs: IceAuthenticationError new ].
	self visitGenericError: anError
]

{ #category : #visiting }
IceLibgitErrorVisitor >> visitGenericError: anError [

	IceGenericError new
		messageText: anError messageText;
		wrappedException: anError;
		signal
]
