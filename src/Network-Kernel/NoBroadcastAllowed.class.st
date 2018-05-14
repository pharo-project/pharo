"
A socket primitive failed due to no broadcast being allowed on this socket.

This failure is fairly specific, as IPv6 does not include broadcast, and it is not supported by the TCP protocol.

Usually, the cause is one of two things;
- We tried sending to the wrong address
- We should have #setOption: 'SO_BROADCAST' value: true on the (UDP) socket
"
Class {
	#name : #NoBroadcastAllowed,
	#superclass : #SocketPrimitiveFailed,
	#category : #'Network-Kernel-Exceptions'
}
