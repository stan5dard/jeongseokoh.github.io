(function () {
    'use strict';
    var search = document.getElementById('publication-search');
    if (!search) return;

    var input = document.getElementById('publication-query');
    var papers = Array.from(document.querySelectorAll('.publication-item'));
    var years = Array.from(document.querySelectorAll('.publication-year'));
    var count = document.getElementById('publication-count');
    var empty = document.getElementById('publication-empty');

    function filterPublications() {
        var terms = input.value.trim().toLocaleLowerCase().split(/\s+/).filter(Boolean);
        var visible = 0;
        papers.forEach(function (paper) {
            var text = paper.dataset.search.toLocaleLowerCase();
            paper.hidden = !terms.every(function (term) { return text.includes(term); });
            if (!paper.hidden) visible += 1;
        });
        years.forEach(function (year) {
            year.hidden = !year.querySelector('.publication-item:not([hidden])');
            var heading = year.querySelector('h2');
            var link = document.querySelector('#navbar-year a[href="#' + heading.id + '"]');
            if (link) link.hidden = year.hidden;
        });
        count.textContent = visible + ' of ' + papers.length + ' publications';
        empty.hidden = visible !== 0;
        // Search results change section positions used by Bootstrap's year navigation.
        if (window.jQuery && window.jQuery.fn.scrollspy) {
            window.jQuery('body').scrollspy('refresh');
        }
    }

    input.addEventListener('input', filterPublications);
    document.getElementById('publication-clear').addEventListener('click', function () {
        input.value = '';
        filterPublications();
        input.focus();
    });
    search.hidden = false;
    filterPublications();
})();
