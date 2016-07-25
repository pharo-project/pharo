enabling/disabling
enable

	Job jobAnnouncer when: JobStart send: #startJob: to: self.
	Job jobAnnouncer when: JobEnd send: #endJob: to: self.
	Job jobAnnouncer when: JobChange send: #updateJob: to: self.
