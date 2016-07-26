I represent key combinations combining a single key + a modifier.

The modifier could be a single modifier key (look at my subclasses) or a combination of them. For example, valid modified key combinations could be:

- shift + a
- ctrl + shift + c

To create a modified key combinations use the #command, #alt, #control or #shift messages. Like for example:

$a command
$b shift
$1 control
$z alt