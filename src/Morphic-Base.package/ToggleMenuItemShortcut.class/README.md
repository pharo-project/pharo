I'm a generic shortcut text to be shown in amenu item. 
I'm generic because there are different strategies to be shown: 

1) Macs want to show symbols (like those weird command and option symbols)
2) Windows wants to show  them as "Alt + Shift + Something"
3) Linux can live with windows scheme

So, my subclasses implements text and symbol strategies. 

IMPORTANT:  I'm assuming the two current existing keyText "disposition" in system: 
- the "old style": just the letters, like 'o, t' or 'b'
- the "full style" (which is in part my fault, from some years ago): something like 'cmd+shift+O+T'.
I format those imputs into something similar (check #expand: method for better explanation).