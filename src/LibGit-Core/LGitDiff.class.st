Class {
	#name : #LGitDiff,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'libgit-calls' }
LGitDiff class >> diff_free: handle [
	
	^ self call: #(void git_diff_free #(void *handle)) options: #()
]

{ #category : #'libgit-finalization' }
LGitDiff class >> freeFunctionSelector [
	^ #diff_free:
]

{ #category : #operations }
LGitDiff >> deltaAt: anInteger [
	"use 1 based offset as in Smalltalk"
	^ LGitDiffDelta fromHandle: (
		self
			diff_get_delta: self
			idx: anInteger - 1) getHandle
]

{ #category : #operations }
LGitDiff >> diffIndexToWorkDir [
	self diffIndexToWorkDirOptions: LGitDiffOptions defaults
]

{ #category : #operations }
LGitDiff >> diffIndexToWorkDirOptions: anLGitDiffOptions [
	self withReturnHandlerDo: [
		self
			diff_index_to_workdir: self
			repo: self repository
			index: self repository index
			opts: anLGitDiffOptions ]
]

{ #category : #operations }
LGitDiff >> diffTree: oldTree toIndex: index [
	
	self
		diffTree: oldTree
		toIndex: index
		options: LGitDiffOptions defaults
]

{ #category : #operations }
LGitDiff >> diffTree: oldTree toIndex: index options: anLGitDiffOptions [
	self withReturnHandlerDo: [
		self
			diff_tree_to_index: self
			repo: self repository
			old_tree: oldTree
			index: index
			opts: anLGitDiffOptions ]
]

{ #category : #operations }
LGitDiff >> diffTree: oldTree toTree: newTree [
	self
		diffTree: oldTree
		toTree: newTree
		options: LGitDiffOptions defaults
]

{ #category : #operations }
LGitDiff >> diffTree: oldTree toTree: newTree options: anLGitDiffOptions [
	self withReturnHandlerDo: [
		self
			diff_tree_to_tree: self
			repo: self repository
			old_tree: oldTree
			new_tree: newTree
			opts: anLGitDiffOptions ]
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_blob_to_buffer: old_blob old_as_path: old_as_path buffer: buffer buffer_len: buffer_len buffer_as_path: buffer_as_path options: options file_cb: file_cb hunk_cb: hunk_cb line_cb: line_cb payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_blob_to_buffer #(LGitBlob old_blob , String old_as_path , String buffer , size_t buffer_len , String buffer_as_path , LGitDiffOptions * options , LGitDiffFileCallback file_cb , LGitDiffHunkCallback hunk_cb , LGitDiffLineCallback line_cb , void * payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_blobs: old_blob old_as_path: old_as_path new_blob: new_blob new_as_path: new_as_path options: options file_cb: file_cb hunk_cb: hunk_cb line_cb: line_cb payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_blobs (LGitBlob old_blob , String old_as_path , LGitBlob new_blob , String new_as_path , LGitDiffOptions *options , LGitDiffFileCallback file_cb , LGitDiffHunkCallback hunk_cb , LGitDiffLineCallback line_cb , void *payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_buffers: old_buffer old_len: old_len old_as_path: old_as_path new_buffer: new_buffer new_len: new_len new_as_path: new_as_path options: options file_cb: file_cb hunk_cb: hunk_cb line_cb: line_cb payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_buffers #(FFIVoid * old_buffer , size_t old_len , String old_as_path , void *new_buffer , size_t new_len , String new_as_path , LGitDiffOptions * options , LGitDiffFileCallback file_cb , LGitDiffHunkCallback hunk_cb , LGitDiffLineCallback line_cb , void * payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_foreach: diff file_cb: file_cb binary_cb: binary_cb hunk_cb: hunk_cb line_cb: line_cb payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_foreach #(self , LGitDiffFileCallback file_cb , LGitDiffBinaryCallback binary_cb , LGitDiffHunkCallback hunk_cb , LGitDiffLineCallback line_cb , void * payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_get_delta: diff idx: idx [
	<todo: 'use LGitDiffDelta* as return type'>
	
	^ self
		call: #(void *git_diff_get_delta #(self , size_t idx))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_get_perfdata: out diff: diff [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_get_perfdata #(LGitDiffPerfData * out , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_get_stats: out diff: diff [
	
	^ self
		call: #(LGitReturnCodeEnum git_diff_get_stats #(LGitDiffStats * out , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_index_to_workdir: diff repo: repo index: index opts: opts [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_index_to_workdir #(LGitDiff * self , LGitRepository repo , LGitIndex index , LGitDiffOptions * opts))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_is_sorted_icase: diff [
	
	^ self call: #(Boolean git_diff_is_sorted_icase #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_num_deltas: diff [
	
	^ self call: #(size_t git_diff_num_deltas #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_num_deltas_of_type: diff type: type [
	
	^ self
		call: #(size_t git_diff_num_deltas_of_type #(self , LGitDeltaTypeEnum type))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_print: diff format: format print_cb: print_cb payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_print #(self , LGitDiffFormatTypeEnum format , LGitDiffLineCallback print_cb , void *payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_status_char: status [
	
	^ self
		call: #(Character git_diff_status_char #(LGitDeltaTypeEnum status))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_tree_to_index: diff repo: repo old_tree: old_tree index: index opts: opts [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_tree_to_index #(LGitDiff * self , LGitRepository repo , LGitTree old_tree , LGitIndex index , LGitDiffOptions * opts))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_tree_to_tree: diff repo: repo old_tree: old_tree new_tree: new_tree opts: opts [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_tree_to_tree #(LGitDiff * diff , LGitRepository repo , LGitTree old_tree , LGitTree new_tree , LGitDiffOptions * opts))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_tree_to_workdir: diff repo: repo old_tree: old_tree opts: opts [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_tree_to_workdir #(LGitDiff * self , LGitRepository repo , LGitTree old_tree , LGitDiffOptions * opts))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiff >> diff_tree_to_workdir_with_index: diff repo: repo old_tree: old_tree opts: opts [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_tree_to_workdir_with_index #(LGitDiff * self , LGitRepository repo , LGitTree old_tree , LGitDiffOptions * opts))
		options: #()
]

{ #category : #enumerating }
LGitDiff >> filesDo: filesBlockOrNil binaryFilesDo: binaryBlockOrNil hunksDo: hunksBlockOrNil linesDo: linesBlockOrNil [
	^ self
		diff_foreach: self
		file_cb: (filesBlockOrNil 
			ifNotNil: [ LGitDiffFileCallback on: filesBlockOrNil ]
			ifNil: [ LGitCallback null ])
		binary_cb: (binaryBlockOrNil 
			ifNotNil: [ 	LGitDiffBinaryCallback on: binaryBlockOrNil ]
			ifNil: [ LGitCallback null ])
		hunk_cb: (hunksBlockOrNil 
			ifNotNil: [ 	LGitDiffHunkCallback on: hunksBlockOrNil ]
			ifNil: [ LGitCallback null ])
		line_cb: (linesBlockOrNil 
			ifNotNil: [ 	LGitDiffLineCallback on: linesBlockOrNil ]
			ifNil: [ LGitCallback null ])
		payload: ExternalAddress null
]

{ #category : #operations }
LGitDiff >> numberOfDeltas [
	^ self diff_num_deltas: self
]

{ #category : #operations }
LGitDiff >> patchAt: anInteger [
	^ LGitPatch
		fromDiff: self
		at: anInteger
]
