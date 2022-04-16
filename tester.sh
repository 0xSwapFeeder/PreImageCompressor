function test_all()
{
    cat "commands_nts/$2.commands" | ./nanotekspice "config_nts/$1.nts" &> "tests_output/$2.out"
    diff "tests_output/$2.out" "expect_output/$2.out" &> /dev/null
    if [ $? -eq 0 ]; then
        echo -e $2 " : \033[32msuccess\033[0m"
    else
        echo -e $2 " : \033[31mfail\033[0m"
    fi
}

### BEFORE TESTS ###
make



## TESTS ###
if [ "$1" -eq "-all" ]; then
    test "simple_or" "simple_or"
    test "self_linked_gates" "self_linked_gates"
    test "4001_nor" "4001_nor"
    test "4011_nand" "4011_nand"
    test "4030_xor" "4030_xor"
    test "4069_not" "4069_not"
    test "4081_and" "4081_and_gate1"
    test "4081_and" "4081_and_gate2"
    test "4081_and" "4081_and_gate3"
    test "4081_and" "4081_and_gate4"
    test "4071_or" "4071_or_gate1"
    test "4071_or" "4071_or_gate2"
    test "4071_or" "4071_or_gate3"
    test "4071_or" "4071_or_gate4"

