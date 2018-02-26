~~ TODO RegExp portion of stdlib which will include commonly used RegExps.

regexp alphaLower;
regexp alphaUpper;
regexp numeric;
regexp alpha;
regexp alphaNumeric
regexp digits;
regexp ascii;
alphaLower = lit 'a' | lit 'b' | lit 'c' | lit 'd' | lit 'e' | lit 'f' | lit 'g' | lit 'h' | lit 'i' | lit 'j' | lit 'k' | lit 'l' | lit 'm' | lit 'n' | lit 'o' | lit 'p' | lit 'q' | lit 'r' | lit 's' | lit 't' | lit 'u' | lit 'v' | lit 'w' | lit 'x' | lit 'y' | lit 'z';
alphaUpper = lit 'A' | lit 'B' | lit 'C' | lit 'D' | lit 'E' | lit 'F' | lit 'G' | lit 'H' | lit 'I' | lit 'J' | lit 'K' | lit 'L' | lit 'M' | lit 'N' | lit 'O' | lit 'P' | lit 'Q' | lit 'R' | lit 'S' | lit 'T' | lit 'U' | lit 'V' | lit 'W' | lit 'X' | lit 'Y' | lit 'Z';
numeric = lit '0' | lit '1' | lit '2' | lit '3' | lit '4' | lit '5' | lit '6' | lit '7' | lit '8' | lit '9';
alpha = alphaLower | alphaUpper;
alphaNumeric = alpha | numeric;
digits = numeric **;
ascii = lit ' ' | lit '!' | lit '"' | lit '#' | lit '$' | lit '%' | lit '&' | lit '\'' | lit '(' | lit ')' | lit '*' | lit '+' | lit ',' | lit '-' | lit '.' | lit '/' | digits | lit ':' | lit ';' | lit '<' | lit '=' | lit '>' | lit '?' | lit '@' | alphaUpper | lit '[' | lit '\' | lit ']' | lit '^' | lit '_' | lit '`' | alphaLower | lit '{' | lit '|' | lit '}' | lit '~' | lit 'DEL' 


isNumeric : (s : string) -> bool {
  return (digits matches s);
}
isAscii : (s : string) -> bool {
	return (ascii matches s);
}
