var url = 'https://www.trivago.com.br/?aDateRange%5Barr%5D=2018-11-26&aDateRange%5Bdep%5D=2018-11-27&aPriceRange%5Bfrom%5D=0&aPriceRange%5Bto%5D=0&iPathId=75911&aGeoCode%5Blat%5D=-12.975613&aGeoCode%5Blng%5D=-38.512547&iGeoDistanceItem=0&aCategoryRange=0%2C1%2C2%2C3%2C4%2C5&aOverallLiking=1%2C2%2C3%2C4%2C5&sOrderBy=relevance%20desc&bTopDealsOnly=false&iRoomType=7&cpt=7591103&iIncludeAll=0&iViewType=0&bIsSeoPage=false&bIsSitemap=false&'
;
var page = new WebPage();
var fs = require('fs');

page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}

