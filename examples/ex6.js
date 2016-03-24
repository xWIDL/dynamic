var x = { a : 1 };

x.a = function(z) {
    return z + 1;
};

var y = x.a(3); // should be 4
