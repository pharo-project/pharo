This class implements the Digital Signature Algorithm (DSA) of the U.S. government's "Digital Signature Standard" (DSS). The DSA algorithm was proposed in 1991 and became a standard in May 1994. The official description is available as a Federal Information Processing Standards Publication (FIPS PUB 186, May 19, 1994). A companion standard, the Secure Hash Standard, or SHS (FIPS PUB 180-1, April 17, 1995), describes a 160-bit message digest algorithm known as the Secure Hash Algorithm (SHA). This message digest is used to compute the document signature.

Here's how to use it:

  1. The "signer" creates a pair of keys. One of these must be kept private. The other may be freely distributed. For example, it could be built into the signature checking code of an application.

  2. When the signer wishes to sign a packet of data (a "message") , he uses the secure hash algorithm to create a 160-bit message digest (hash) which is used as the input to DSA. The result of this is a pair of large numbers called a "signature" that is attached to the original message.

  3. When someone receives a signed message purported to have come from the signer, they compute the 160-bit hash of the message and pass that, along with the message signature and the signer's public key, to the signature verification algorithm. If the signature checks, then it is virtually guaranteed that the message originated from someone who had the signer's private key. That is, the message is not a forgery and has not been modified since it was signed. For example, if the message contains a program, and the recipient trusts the signer, then the recipient can run the program with the assurance that it won't do anything harmful. (At least, not intentionally. A digital signature is no guarantee against bugs! :->)

The signer must keep the private key secure, since anyone who has the private key can forge the signer's signature on any message they like. As long as the secret key is not stolen, cryptographers believe it to be virtually impossible either to forge a signature, to find a message that matches an existing sigature, or to discover the signer's private key by analyzing message signatures. Knowing the public key (which, for example, could be recovered from an application that had it built in), does not weaken the security at all.

An excellent reference work on digital signatures and cryptography in general is:

  Schneier, Bruce
  "Applied Cryptography: Protocols, Algorithms, and Source Code in C"
  John Wiley and Sons, 1996.

I used this book as a guide to implementing many of the numerical algorithms required by DSA.

Patents and Export Restrictions:

Many digital signature technologies are patented. DSA is also patented, but the patent is owned by the U.S. government which has made DSA available royalty-free. There is a claim that the government patent infringes on an earlier patent by Schnorr, but the government is requiring the use of DSA, so they apparently believe this claim is not strong enough to be a serious threat to their own patent.

Most cryptography technology, including digital signature technology, requires an export license for it to be distributed outside the U.S. Recent legislation may have relaxed the export license requirements, but it would be prudent to check the current regulations before exporting this code.