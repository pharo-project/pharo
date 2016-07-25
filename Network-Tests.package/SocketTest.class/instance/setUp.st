setup
setUp

	listenerSocket := Socket newTCP listenOn: self listenerPort backlogSize: 4 interface: self listenerAddress.
