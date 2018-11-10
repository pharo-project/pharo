Class {
	#name : #LGitPatch,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'libgit-calls' }
LGitPatch class >> diff_patch_free: handle [
	
	^ self call: #(void git_patch_free #(void *handle)) options: #()
]

{ #category : #'libgit-finalization' }
LGitPatch class >> freeFunctionSelector [
	^ #diff_patch_free:
]

{ #category : #'instance creation' }
LGitPatch class >> fromDiff: aDiff at: anInteger [
	^ (self of: aDiff repository)
		fromDiff: aDiff at: anInteger;
		yourself
]

{ #category : #operations }
LGitPatch >> fromDiff: aDiff at: anInteger [
	"use 1 based offset as in Smalltalk"
	self withReturnHandlerDo: [
		self
			patch_from_diff: self
			diff: aDiff
			idx: anInteger - 1 ].
	self autoRelease
]

{ #category : #operations }
LGitPatch >> hunkAt: anInteger [
	| hunk lines |
	hunk := LGitDiffHunk externalNew.
	lines := LGitWriteBackValueSizeT new.
	self withReturnHandlerDo: [
		self
			patch_get_hunk: hunk
			lines_in_hunk: lines patch: self
			hunk_idx: anInteger ].
		
	^ hunk
]

{ #category : #'libgit-calls' }
LGitPatch >> patch_from_blob_and_buffer: out old_blob: old_blob old_as_path: old_as_path buffer: buffer buffer_len: buffer_len buffer_as_path: buffer_as_path opts: opts [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_patch_from_blob_and_buffer #(LGitPatch * self , LGitBlob old_blob , String old_as_path , String buffer , FFIUInt32 buffer_len , String buffer_as_path , LGitDiffOptions * opts))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPatch >> patch_from_blobs: out old_blob: old_blob old_as_path: old_as_path new_blob: new_blob new_as_path: new_as_path opts: opts [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_patch_from_blobs #(LGitPatch * self , LGitBlob old_blob , String old_as_path , LGitBlob new_blob , String new_as_path , LGitDiffOptions * opts))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPatch >> patch_from_buffers: out old_buffer: old_buffer old_len: old_len old_as_path: old_as_path new_buffer: new_buffer new_len: new_len new_as_path: new_as_path opts: opts [
	self flag: #todo. "This one doesnt seems to exist anymore. Instead I think it is git_patch_from_blob_and_buffer"
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_patch_from_blobuffers #(LGitPatch * self , FFIVoid * old_buffer , FFIUInt32 old_len , String old_as_path , String new_buffer , FFIUInt32 new_len , String new_as_path , LGitDiffOptions * opts))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPatch >> patch_from_diff: out diff: diff idx: idx [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_patch_from_diff #(LGitPatch * self , LGitDiff diff , size_t idx))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPatch >> patch_get_delta: patch [
	
	^ self
		call: #(LGitDiffDelta * git_patch_get_delta #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPatch >> patch_get_hunk: out lines_in_hunk: lines_in_hunk patch: patch hunk_idx: hunk_idx [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_patch_get_hunk #(
				LGitDiffHunk **out , 
				LGitWriteBackValueSizeT *lines_in_hunk, 
				LGitPatch *patch, 
				size_t hunk_idx))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPatch >> patch_get_line_in_hunk: out patch: patch hunk_idx: hunk_idx line_of_hunk: line_of_hunk [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_patch_get_line_in_hunk #(LGitDiffLine * out , self , size_t hunk_idx , size_t line_of_hunk))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPatch >> patch_print: patch print_cb: print_cb payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_patch_print #(self , LGitDiffLineCallback print_cb , void *payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #printing }
LGitPatch >> printDiff [
	^ String new: 500 streamContents: [ :stream |
		self printDiffOn: stream ]
]

{ #category : #operations }
LGitPatch >> printDiffOn: aStream [
	| callback |
	callback := LGitDiffLineCallback on: [ :delta :hunk :line :payload |
		aStream nextPutAll: line content. 0 ].
	
	self withReturnHandlerDo: [
		self
			patch_print: self
			print_cb: callback
			payload: nil ]
]
