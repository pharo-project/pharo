parameters
parameter46Documentation

"
46      size of machine code zone, in bytes

To speed-up execution, the cog uses internally a JIT compiler which translates to machine code frequently used bytecoded method and run the machine code to execute them instead of using the interpreter. The machine code zone is an executable memory zone containing all the machine code versions of methods. 

The default size should be between 1Mb and 2Mb for standard applications. If memory is a constraint, it can be lowered down to 750kb, under that you should use the stack VM else the performance starts to drastically decrease.

This setting is useful to tune your application performance. For example, on compilation-intensive benchs, 750kb is the optimal machine code zone size. On large benchmarks, 2 Mb, maybe more, is better, but then one may see machine code garbage collection pauses. On small benchs all the methods fit into this zone so it doesn't really matter.

Growing the machine code zone:
- increase the number of methods that can be present there, hence decreasing machine code zone garbage collection frequency and method jit compilation.
- decrease the machine code zone to cpu instruction cache mapping
- slow down machine code zone garbage collection

To set the machine code zone you need to use another parameter (47 I think) and restart the VM+image.
"