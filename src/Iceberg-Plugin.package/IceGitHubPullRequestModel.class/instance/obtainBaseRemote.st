private
obtainBaseRemote
	| base |
	base := self gitBase.
	^ IceRemote url: ((base at: #'git_url') copyReplaceAll: 'git://' with: 'https://')