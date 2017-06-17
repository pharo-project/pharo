libgit-calls
merge_heads: heads length: length merge_options: mergeOptions checkout_options: checkoutOptions
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_merge #(
				self , 
				FFIExternalArray heads ,
				int length , 
				LGitMergeOptions * mergeOptions ,
				LGitCheckoutOptions * checkoutOptions))
		options: #()