I represent a loop in the bytecode.

My instances represent an unconditional loop. These loops can't fall through the code right after as it's infinite, however, it can finish if there is a return inside.

Conditional loops are instances of my subclass.

backjump <Smi> pc of the backjump instruction