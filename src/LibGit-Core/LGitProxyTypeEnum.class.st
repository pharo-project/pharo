"
/**
 * The type of proxy to use.
 */
"
Class {
	#name : #LGitProxyTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitProxyTypeEnum class >> enumDecl [
	^ #(
	"/**
	 * Do not attempt to connect through a proxy
	 *
	 * If built against libcurl, it itself may attempt to connect
	 * to a proxy if the environment variables specify it.
	 */"
	GIT_PROXY_NONE 0
	"/**
	 * Try to auto-detect the proxy from the git configuration.
	 */"
	GIT_PROXY_AUTO 1
	"/**
	 * Connect via the URL given in the options
	 */"
	GIT_PROXY_SPECIFIED 2
	)
]

{ #category : #'accessing-values' }
LGitProxyTypeEnum class >> git_proxy_auto [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitProxyTypeEnum class >> git_proxy_none [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitProxyTypeEnum class >> git_proxy_specified [
	^ self basicNew
		value: 2;
		yourself
]
