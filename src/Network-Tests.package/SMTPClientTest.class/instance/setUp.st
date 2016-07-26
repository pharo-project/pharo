running
setUp
	socket := MockSocketStream on: ''.
	smtp := SMTPClient new.
	smtp stream: socket.