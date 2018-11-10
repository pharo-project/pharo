"
/** Memory representation of a set of config files */
typedef struct git_config git_config;

https://libgit2.github.com/libgit2/#HEAD/type/git_config

"
Class {
	#name : #LGitConfig,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'libgit-calls' }
LGitConfig class >> config_free: handle [
	^ self
		call: #(void git_config_free #(void *handle))
		options: #()
]

{ #category : #'libgit-finalization' }
LGitConfig class >> freeFunctionSelector [
	^ #config_free:
]

{ #category : #'libgit-calls' }
LGitConfig >> config_delete_entry: name [
	^ self call:
			#(LGitReturnCodeEnum git_config_delete_entry(
				self, 
				String name))
		options: #()
]

{ #category : #'libgit-calls' }
LGitConfig >> config_find_global_path: out [
	^ self call: #(int git_config_find_global(String* out))
]

{ #category : #'libgit-calls' }
LGitConfig >> config_get_bool: name to: out [
	^ self call:
			#(LGitReturnCodeEnum git_config_get_bool(
				int *out,
				self, 
				String name))
		options: #()
]

{ #category : #'libgit-calls' }
LGitConfig >> config_get_string: name output: output [
	^ self call:
			#(LGitReturnCodeEnum git_config_get_string_buf(
				LGitBuf* output, 
				self, 
				String name))
		options: #()
]

{ #category : #'libgit-calls' }
LGitConfig >> config_open_global: out [
	^ self call: #(int git_config_open_global("LGitConfig **"void **out, self))
]

{ #category : #'libgit-calls' }
LGitConfig >> config_set_bool: name to: value [
	^ self call:
			#(LGitReturnCodeEnum git_config_set_bool(
				self, 
				String name, 
				Boolean value))
		options: #()
]

{ #category : #'libgit-calls' }
LGitConfig >> config_set_string: name to: value [
	^ self call:
			#(LGitReturnCodeEnum git_config_set_string(
				self, 
				String name, 
				String value))
		options: #()
]

{ #category : #accessing }
LGitConfig >> email [
	
	^ self getString: 'user.email' ifAbsent: [ NotFound signalFor: 'user.email' ]
]

{ #category : #accessing }
LGitConfig >> email: aString [ 
	
	self setString: 'user.email' to: aString
]

{ #category : #accessing }
LGitConfig >> getBoolean: name [
	| out |
	
	out := ByteArray new: FFIInt32 externalTypeSize.
	self config_get_bool: name to: out.
	^ (out signedLongAt: 1) ~= 0
]

{ #category : #accessing }
LGitConfig >> getString: name ifAbsent: aBlock [
	^ self getString: name ifPresent: #yourself ifAbsent: aBlock
]

{ #category : #accessing }
LGitConfig >> getString: name ifPresent: presentBlock ifAbsent: absentBlock [
	| buf |
	buf := LGitBuf new.
	^ [
		self config_get_string: name output: buf.
		buf prim_ptr readString 
			ifNotEmpty: presentBlock
			ifEmpty: absentBlock
	]
	ensure: [ buf free ]


]

{ #category : #accessing }
LGitConfig >> globalConfigurationPath [
	| outHandle |

	outHandle := ByteString new: 256.
	self config_find_global_path: outHandle.
	^ outHandle
]

{ #category : #accessing }
LGitConfig >> openGlobal [
	| outHandle |

	outHandle := ExternalAddress null.
	self config_open_global: outHandle.
	^ (self class fromHandle: outHandle) autoRelease
	
]

{ #category : #accessing }
LGitConfig >> setBoolean: name to: value [
	self config_set_bool: name to: value
]

{ #category : #accessing }
LGitConfig >> setString: name to: value [
	self config_set_string: name to: value
]

{ #category : #accessing }
LGitConfig >> unset: name [
	^ self config_delete_entry: name
]

{ #category : #accessing }
LGitConfig >> username [
	
	^ self getString: 'user.name' ifAbsent: [ NotFound signalFor: 'user.name' ]
]

{ #category : #accessing }
LGitConfig >> username: aString [ 
	
	self setString: 'user.name' to: aString
]
