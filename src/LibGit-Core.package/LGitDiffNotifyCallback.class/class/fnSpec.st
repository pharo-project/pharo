callback spec
fnSpec
	^#(FFIInt32 (LGitDiff * diff_so_far, LGitDiffDelta * delta_to_add, FFIExternalString matched_pathspec, void * payload))