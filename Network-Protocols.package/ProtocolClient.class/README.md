ProtocolClient is the abstract super class for a variety of network protocol clients.
It uses a stream rather than the direct network access so it could also work for streams on serial connections etc.

Structure:
	stream				stream representing the connection to and from the server
	connectInfo			information required for opening a connection
	lastResponse			remembers the last response from the server.
	progressObservers 	any object understanding #show: can be registered as a progress observer (login, transfer, etc)