Class {
	#name : #LGitRemoteAutotagOptionTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitRemoteAutotagOptionTypeEnum class >> enumDecl [
	^ #(
		"
		 * Use the setting from the configuration.
		 "
		GIT_REMOTE_DOWNLOAD_TAGS_UNSPECIFIED 0
		"
		 * Ask the server for tags pointing to objects we're already
		 * downloading.
		 "
		GIT_REMOTE_DOWNLOAD_TAGS_AUTO 1
		"
		 * Don't ask for any tags beyond the refspecs.
		 "
		GIT_REMOTE_DOWNLOAD_TAGS_NONE 2
		"
		 * Ask for the all the tags.
		 "
		GIT_REMOTE_DOWNLOAD_TAGS_ALL 3
	)
]

{ #category : #'accessing-values' }
LGitRemoteAutotagOptionTypeEnum class >> git_remote_download_tags_all [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitRemoteAutotagOptionTypeEnum class >> git_remote_download_tags_auto [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitRemoteAutotagOptionTypeEnum class >> git_remote_download_tags_none [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitRemoteAutotagOptionTypeEnum class >> git_remote_download_tags_unspecified [
	^ self basicNew
		value: 0;
		yourself
]
