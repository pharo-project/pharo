I am ZdcAbstractSocketStream, a binary read/write stream for socket communication.

Interally, IO is done through a read and a write ZdcIOBuffer.

I am abstract, my subclasses should implement actual IO through a delegate.