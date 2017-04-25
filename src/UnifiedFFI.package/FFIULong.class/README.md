I'm an unsigned long.
Longs are different from other types because they can have different sizes depending on the architecture, reason why they need a special treatment (and we cannot reduce them to a int32 or int64 type). 
For example, this are the sizes according architecture: 

- i386: 4 bytes
- x86_64 SystemV: 8 bytes
- x86_64 Windows: 4 bytes

So we model long as a special type and we use platform speciphic settings.