Copyright 2008 Cadence Design Systems, Inc.
   
   Licensed under the Apache License, Version 2.0 (the ''License''); you may not use this file except in compliance with the License.  You may obtain a copy of the License at  http://www.apache.org/licenses/LICENSE-2.0

Instances of Alien represent actual parameters, return results and function pointers in FFI call-outs and call-backs and provide handles on external data.  See NewsqueakIA32ABIPlugin for the VM code that actually implements call-outs and call-backs.

See the class-side examples category for some simple example workspaces.

Aliens represent ABI (C language) data.  They can hold data directly in their bytes or indirectly by pointing to data on the C heap.  Alien instances are at least 5 bytes in length. The first 4 bytes of an Alien hold the size, as a signed integer, of the datum the instance is a proxy for.  If the size is positive then the Alien is "direct" and the actual datum resides in the object itself, starting at the 5th byte.  If the size is negative then the proxy is "indirect", is at least 8 bytes in length and the second 4 bytes hold the address of the datum, which is assumed to be on the C heap.  Any attempt to access data beyond the size will fail.  If the size is zero then the Alien is a pointer, the second 4 bytes hold a pointer, as for "indirect" Aliens, and accessing primitives indirect through the pointer to access data, but no bounds checking is performed.

When Aliens are used as parameters in FFI calls then all are "passed by value", so that e.g. a 4 byte direct alien will have its 4 bytes of data passed, and a 12-byte indirect alien will have the 12 bytes its address references passed.  Pointer aliens will have their 4 byte pointer passed.  So indirect and pointer aliens are equivalent for accessing data but different when passed as parameters, indirect Aliens passing the data and pointer Aliens passing the pointer.

Class Variables:
GCMallocedAliens <AlienWeakTable of <Alien -> Integer>> - weak collection of malloced aliens, used to free malloced memory of Aliens allocated with newGC:
LoadedLibraries <Dictionary of <String -> Alien>> - library name to library handle map