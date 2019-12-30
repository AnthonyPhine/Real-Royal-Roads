# GNU Octave script

dual_data = [15,  52,   148,   104,   212,   1070,   2433,   1400,   3021,   14858,   27705,   20444,   41051  ];
roul_data = [9.4, 63.7, 231.4, 94.3,  101.5, 1688.1, 2650.1, 1442.2, 1793.9, 19994.4, 42794,   23487.1, 40011.1];
bolt_data = [6.8, 79.5, 263.2, 127.1, 225.6, 1593.9, 2021.7, 1358.4, 3395,   23213.1, 27341.8, 16190,   39066.2];
tour_data = [6.5, 51.7, 85.6,  133.8, 158.4, 1451.3, 2550.5, 1161,   3069.5, 15982.1, 31191,   26725.4, 47940.9];

all_data = [tour_data; roul_data; bolt_data];
data = mean(all_data, 1);

x = [4:1:16];
p = polyfit (x, data, 5);
xfit = 4:16;
yfit = polyval(p, xfit);


fplot ("n^3 * (n / 2)", [4, 16], '-k')
hold on

plot ([4:1:16], tour_data, ':xr')
plot ([4:1:16], roul_data, ':xg')
plot ([4:1:16], bolt_data, ':xb')
plot (xfit,     yfit,      '-y' )

l = legend ("n^3 (n/2)",
            "Tournament",
            "Roulette",
            "Boltzmann",
            "Overall")

legend(l, "location", "northwest")
