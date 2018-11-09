X="hey"
function printX {
    echo $X
}
function localX {
    local X="David is so cool"
    printX
}
printX
localX
printX
