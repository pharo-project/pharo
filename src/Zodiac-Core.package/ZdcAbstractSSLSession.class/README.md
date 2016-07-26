I am ZdcAbstractSSLSession, an object managing the secure communication between two parties.

I define the abstract interface for my subclasses.

More specifically, I handle connection setup handshaking as well as the encryption
and decryption of data travelling between two parties.

Apart from instanciating and later explicitely destroying me, I am used by feeding data 
into me using the methods in my operations protocol. These might result in data that
has to be sent to the other side.

I am propably too primitive to be used directly, see ZdcSecureSocketStream for a higher level client.