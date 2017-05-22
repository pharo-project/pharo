private
createPackForReference: aReference
	| odb walk packbuilder writepack stats packbuilderPayload progresscb packbuilderCallback |
	odb := self database.
	walk := (LGitRevwalk of: self)
		beSortedByCommitTime;
		pushReference: aReference;
		yourself.
	packbuilder := LGitPackBuilder new.
	packbuilder packbuilder_new: packbuilder repo: self.	"hideCallback := LG2RevwalkHideCb on: [ :oid :payload |  ].
	LG2RevwalkH uniqueInstance
		revwalk_add_hide_cb: walk
		hide_cb: hideCallback
		payload: nil."
	[ walk hasNext ] whileTrue: [ 
		| commit tree |
		commit := walk next.
		tree := commit tree.
		packbuilder
			packbuilder_insert_commit: packbuilder id: commit id;
			packbuilder_insert_tree: packbuilder id: tree id.
		commit free.
		tree free ].
	progresscb := LGitTransferProgressCallback on: [ :progress :data | 0 ].
	writepack := LGitOdbWritepack externalNew.
	writepack
		odb_write_pack: writepack
		db: odb
		progress_cb: progresscb
		progress_payload: ExternalAddress null.
	writepack := LGitOdbWritepack fromHandle: writepack getHandle pointer.
	stats := LGitTransferProgress externalNew.
	packbuilderPayload := LGitPackbuilderForeachPayload externalNew.
	packbuilderCallback := LGitPackbuilderForeachCallback
		on: [ :buffer :length :payload | payload writepack appendWithBuffer: buffer length: length progress: payload stats ].
	packbuilderPayload
		writepack: writepack;
		stats: stats.
	packbuilder packbuilder_foreach: packbuilder cb: packbuilderCallback payload: packbuilderPayload.
	writepack commitWithProgress: stats