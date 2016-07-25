private
setServerSocketOptions: socket
	socket
		setOption: 'TCP_NODELAY' value: 1;
		setOption: 'SO_SNDBUF' value: self class socketBufferSize;
		setOption: 'SO_RCVBUF' value: self class socketBufferSize 