private
subscribeToJobAnnouncer

	{	JobStart -> #jobStarted:.
		JobEnd -> #jobEnded:.
	} do: [ :pair |
		jobAnnouncer weak
			when: pair key 
			send: pair value 
			to: self ].
