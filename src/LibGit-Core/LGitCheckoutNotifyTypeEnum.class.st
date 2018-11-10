Class {
	#name : #LGitCheckoutNotifyTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitCheckoutNotifyTypeEnum class >> enumDecl [
	^#(
	GIT_CHECKOUT_NOTIFY_NONE 0
	GIT_CHECKOUT_NOTIFY_CONFLICT 1
	GIT_CHECKOUT_NOTIFY_DIRTY 2
	GIT_CHECKOUT_NOTIFY_UPDATED 4
	GIT_CHECKOUT_NOTIFY_UNTRACKED 8
	GIT_CHECKOUT_NOTIFY_IGNORED 16
	GIT_CHECKOUT_NOTIFY_ALL 65535
)
]

{ #category : #'accessing-values' }
LGitCheckoutNotifyTypeEnum class >> git_checkout_notify_all [
	^ self basicNew
		value: 65535;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutNotifyTypeEnum class >> git_checkout_notify_conflict [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutNotifyTypeEnum class >> git_checkout_notify_dirty [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutNotifyTypeEnum class >> git_checkout_notify_ignored [
	^ self basicNew
		value: 16;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutNotifyTypeEnum class >> git_checkout_notify_none [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutNotifyTypeEnum class >> git_checkout_notify_untracked [
	^ self basicNew
		value: 8;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutNotifyTypeEnum class >> git_checkout_notify_updated [
	^ self basicNew
		value: 4;
		yourself
]
