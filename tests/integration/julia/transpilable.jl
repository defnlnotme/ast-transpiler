@kwdef struct Second
    myClassProperty::String = raw"classProp"
    myBoolProp::Bool = false
    stringifyNumber::Function = stringifyNumber
end
function stringifyNumber(self::Second, arg)
    return string(arg);

end

@kwdef struct Test
    test::Function = test
end
function test(self::Test, )
    a = 1;
    
b = 2;
    
c = a + b;
    
println(c); # should print 3
    
s1 = raw"a";
    
s2 = raw"b";
    
s3 = string(s1, s2);
    

    
stringVar = raw"hello";
    
println(stringVar); # should print "hello"
    
println(s3); # should print "ab"
    
x = false;
    
    if x
        println(raw"x is true");
    else
        println(raw"x is false"); # should print "x is false"
    end

    
instance = Second();
    
println(instance.stringifyNumber(instance, 4)); # should print 4
    
println(instance.myClassProperty); # should print "classProp"
    
    if instance.myBoolProp == false
        println(raw"myBoolProp is false"); # should print "myBoolProp is false"
    end

    
arr = [1, 2, 3, 4];
    
println(length(arr)); # should print 4
    
first = arr[1];
    
println(first); # should print 1
    
dict = Dict(
    :a => raw"b"
);
    
println(dict[:a]); # should print "b"
    
i = 0;
    
    for w in 0:9
        i = i + 1;
    end
    
println(string(i)); # should print 10
    
list2 = [1, 2, 3, 4, 5];
    
reverse!(list2);
    
println(list2[1]); # should print 5
    
    #should delete key from dict
dict2 = Dict(
    :a => 1,
    :b => 2
);
    
delete!(dict2, :a);
    
dictKeys = collect(keys(dict2));
    
println(length(dictKeys)); # should print 1
    
println(dictKeys[1]); # should print "b"
    
firstConcat = [raw"a", raw"b"];
    
secondConcat = [raw"c", raw"d"];
    
both = concat(firstConcat, secondConcat);
    
println(length(both)); # should print 4
    
println(both[3]); # should print "c"
    
baseString = raw"aabba";
    
replacedAllString = replace(baseString, raw"a" => raw"");
    
println(replacedAllString); # should print "bb"

end
