private
subscribeToJobAnnouncer

	{	JobStart -> #jobStarted:.
		JobEnd -> #jobEnded:.
	} do: [ :pair |
		jobAnnouncer
			when: pair key 
			send: pair value 
			to: self ].
