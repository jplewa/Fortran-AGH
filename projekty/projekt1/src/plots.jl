# This program plots errors from files located in ./out/results/ (specific file names are required)
# and saves them in ./out/plots/.
# No error checking is performed since the main Fortran program generates files with errors for the
# three floating point precisions for N values in the same range.


# Functions for axis label formatting and scaling the plots.
# This might seem redundant, but I couldn't get the Y axis to look the way I wanted 
# especially for kind 16 precision (as the errors were very small) with any of the Julia plotting libraries

function get_scaling_factor(A :: Array{Float64, 1})
    i = 0 :: Integer
    while !any(map(x -> (x > Float64(1)), A))
        A = map(x -> Float64(x)*Float64(10.0), A)
        i += 1
    end
    i
end

function get_superscript(i)
    suf = ""
    while (i > Integer(0))
        j = rem(i,10)
        i = div(i,10)
        if j == 1
            suf = string(suf, '\U000B9')
        elseif j == 2
            suf = string(suf,'\U000B2')
        elseif j == 3
            suf = string(suf, '\U000B3')
        else
            suf = string(suf, '\U02070'+j)
        end
    end
    suf = string(suf, '\U0207B')
    suf = reverse(suf)
end

fx = open("out/results/xAxis")
fy4 = open("out/results/kind4")
fy8 = open("out/results/kind8")
fy16 = open("out/results/kind16")
xAxis = map(x -> parse(Float64,x), readlines(fx))
yAxis4 = map(x -> parse(Float64,x), readlines(fy4))
yAxis8 = map(x -> parse(Float64,x), readlines(fy8))
yAxis16 = map(x -> parse(Float64,x), readlines(fy16))
close(fx)
close(fy4)
close(fy8)
close(fy16)
i = get_scaling_factor(yAxis4)
suf = get_superscript(i)
yAxis4 = map(x -> Float64(x)*Float64(10)^i, yAxis4)
yAxis8 = map(x -> Float64(x)*Float64(10)^i, yAxis8)
yAxis16 = map(x -> Float64(x)*Float64(10)^i, yAxis16)
using Plots
plotlyjs()
using Formatting
plot(xAxis, yAxis4, label = "kind 4", yformatter = :scientific, title = "Average error", yformatter = x -> format(Float64(x), suffix = string("×10",suf), precision = 1))
plot!(xAxis, yAxis8, label="kind 8")
plot!(xAxis, yAxis16, label="kind 16")
using Rsvg
savefig("out/plots/kind_4_8_16.pdf")

fy8 = open("out/results/kind8")
fy16 = open("out/results/kind16")
yAxis8 = map(x -> parse(Float64,x), readlines(fy8))
yAxis16 = map(x -> parse(Float64,x), readlines(fy16))
close(fy8)
close(fy16)
i = get_scaling_factor(yAxis8)
suf = get_superscript(i)
yAxis8 = map(x -> Float64(x)*Float64(10)^i, yAxis8)
yAxis16 = map(x -> Float64(x)*Float64(10)^i, yAxis16)
using Plots
plotlyjs()
plot(xAxis, yAxis8, label = "kind 8", yformatter = :scientific, title = "Average error", yformatter = x -> format(Float64(x), suffix = string("×10",suf), precision = 1))
plot!(xAxis, yAxis16, label="kind 16")
using Rsvg
savefig("out/plots/kind_8_16.pdf")

fy16 = open("out/results/kind16")
yAxis16 = map(x -> parse(Float64,x), readlines(fy16))
close(fy16)
i = get_scaling_factor(yAxis16)
suf = get_superscript(i)
yAxis16 = map(x -> Float64(x)*Float64(10)^i, yAxis16)
using Plots
plotlyjs()
plot(xAxis, yAxis16, label="kind 16", title="Average error", yformatter = x -> format(Float64(x), suffix = string("×10",suf), precision = 1))
using Rsvg
savefig("out/plots/kind_16.pdf")
