libgit-calls
remote_update_tips: remote callbacks: remoteCallbacks update_fetchhead: update_fetchhead download_tags: download_tags reflog_message: reflog_message
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_update_tips #(self , LGitRemoteCallbacks * remoteCallbacks , Boolean update_fetchhead , LGitRemoteAutotagOptionTypeEnum download_tags , String reflog_message))
		options: #(optStringOrNull)