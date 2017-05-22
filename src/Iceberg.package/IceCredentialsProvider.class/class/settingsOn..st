settings
settingsOn: aBuilder 
	<systemsettings>
	(aBuilder setting: #useCustomSsh)
		parent: #Iceberg;
		noOrdering;
		target: self;
		label: 'Use custom SSH keys';
		description: 'Check if you want to have your own pair of keys (id_rsa and id_rsa.pub), without using the ssh-agent';
		with: [
			(aBuilder setting: #username)
				target: self;
				targetSelector: #sshCredentials;
				label: 'SSH Username';
				description: 'If you are trying to connect to github through ssh (something like git@github.com:owner/projectName), the username should be `git`'.

			(aBuilder setting: #publicKey)
				target: self;
				targetSelector: #sshCredentials;
				type: #FilePathEncoder;
				label: 'Public SSH key';
				description: 'The path to your private ssh key, probably /home/<userName>/.ssh/id_rsa.pub'.

			(aBuilder setting: #privateKey)
				target: self;
				targetSelector: #sshCredentials;
				type: #FilePathEncoder;
				label: 'Private SSH key';
				description: 'The path to your private ssh key, probably /home/<userName>/.ssh/id_rsa'.

			(aBuilder setting: #keyPassphrase)
				target: self;
				targetSelector: #sshCredentials;
				label: 'Passphrase of your SSH key'.

		].
	
	(aBuilder group: #'Plaintext Credentials')
		parent: #Iceberg;
		noOrdering;
		with: [ 
			(aBuilder setting: #username)
				target: self;
				targetSelector: #plaintextCredentials;
				label: 'Github username';
				description: 'Your github username'.

			(aBuilder setting: #password)
				target: self;
				targetSelector: #plaintextCredentials;
				type: #Password;
				label: 'Your github password, if you want Iceberg to remember it, but be aware that this does not pretend to be safe.'.
		]