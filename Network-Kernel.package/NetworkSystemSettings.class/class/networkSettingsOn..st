settings
networkSettingsOn: aBuilder 
	<systemsettings> 
	(aBuilder group: #network) 
		label: 'Network';
		description: 'All network related settings';
		noOrdering;
		with: [ 
			(aBuilder setting: #useHTTPProxy)      
				label: 'Use HTTP proxy' ;   
				noOrdering;
		 		description: 'If checked then the you will be able to set a port number and a server name. If unchecked, then no http proxy is used.';
				with: [
					(aBuilder setting: #httpProxyPort)
						label: 'Port' ;
						description: 'The HTTP proxy port'.
					(aBuilder setting: #httpProxyServer)
						label: 'Server' ;
						description: 'The HTTP proxy server (i.e. proxy.univ-brest.fr)'.
					(aBuilder setting: #manageHttpProxyExceptions)
						label: 'Exceptions' ;
						target: NetworkSystemSettings;
						description: 'Domains to avoid to proxy (separated by ;)'
				].
			(aBuilder setting: #useNetworkAuthentification) 
				label: 'Proxy authentication' ;
				noOrdering;
		 		description: 'If checked then you will be able to enter a user name and a password for network authentification. Store  HTTP 1.0 basic authentication credentials. Note: this is an ugly hack that stores your password in your image.  It''s just enought to get you going if you use a firewall that requires authentication';
				with: [
					(aBuilder setting: #proxyUser)
						label: 'User name' ;
						description: 'The proxy user name'.
					(aBuilder setting: #proxyPassword)
						type: #Password;
						label: 'Password' ;
						description: 'The user password'].
			(aBuilder setting: #blabEmail)
				label: 'Blab email' ;
				ghostHelp: 'somebody@some.where';
				description: 'Enter blab email of the form ''somebody@some.where''. It can be empty']