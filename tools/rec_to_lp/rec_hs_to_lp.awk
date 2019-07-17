BEGIN {
    special["="] = 1
    special["main"] = 1
    special["do"] = 1
    special["data"] = 1
    special["|"] = 1
}
/::/ {
    sub(/::/, ":") ;
    r = gensub(/^([^:]+) : (.*)$/,
               "symbol \\1 : \\2", "1") ;
    context[$1] = 1 ;
    q = gensub(/->/, "⇒", "g", r) ;
    print q
}
/print/ { sub(/print/, "compute") ; print }

## Take a dirty identifier (i.e. possibly with a surrounding
## parenthesis), cleans it and determine whether it is a variable
function is_var(ident) {
    # Remove parens around field
    clean = gensub(/\(?(\w+)\)?/, "\\1", "1", $i) ;
    # If the first letter is uppercase, identifier is a constructor
    # defined by a 'data'
    first_up = match(clean, /[A-Z]/) ;
    is_constructor = first_up == 1 ;
    is_special = clean in special ;
    is_defined = clean in context ;
    return !is_constructor && !is_special && !is_defined
}
/=/ {
    for (i = 1; i <= NF; i++) {
        if (is_var($i))
            $i = "\&"$i
    }
    t = gensub(/^([^=]+) = (.*)$/,
               "rule \\1 → \\2", "1") ;
    print t
}
/main → do/ { }
END {}
